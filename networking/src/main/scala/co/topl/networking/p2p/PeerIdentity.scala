package co.topl.networking.p2p

import cats.implicits._
import cats.data.{EitherT, OptionT}
import cats.effect.{Async, Resource}
import cats.effect.std.Random
import co.topl.crypto.signing.Ed25519
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.net.Socket

object PeerIdentity {

  /**
   * Performs a Peer-ID handshake with the remote peer to determine their "Peer ID", which is just an Ed25519 VK.
   * 1. Exchange VKs
   * 2. Exchange random 32-byte challenges
   * 3. Sign the remote peer's challenge
   * 4. Exchange signatures
   * 5. Verify remote signature satisfies the locally-generated challenge with the peer's claimed VK
   *
   * @param localPeerSK A secret key which can generate an identity as well as prove ownership of the identity
   * @return A function which uses a Socket to return Either a failure or (the remote peer's ID, true if peer is on newer protocol versions)
   */
  def extractor[F[_]: Async: Random](
    localPeerSK:     Ed25519.SecretKey,
    ed25519Resource: Resource[F, Ed25519]
  ): F[Socket[F] => F[Either[ExtractionException, (ByteString, Boolean)]]] =
    ed25519Resource
      .use(e => Async[F].delay(e.getVerificationKey(localPeerSK)))
      .map(_.bytes)
      .map(ByteString.copyFrom)
      .map(localPeerVK =>
        (socket: Socket[F]) =>
          (
            for {
              _ <- EitherT.liftF(socket.write(Chunk.byteBuffer(localPeerVK.asReadOnlyByteBuffer())))
              remoteVK <- OptionT(socket.read(32))
                .toRight(ExtractionException.VKNotProvided)
                .leftWiden[ExtractionException]
                .map(_.toArray)
              remoteVKBS = ByteString.copyFrom(remoteVK)
              localChallenge <- EitherT.liftF(Random[F].nextBytes(16).map(PostLegacyChallengePrefix ++ _))
              _              <- EitherT.liftF(socket.write(Chunk.array(localChallenge)))
              remoteChallenge <- OptionT(socket.read(32))
                .toRight(ExtractionException.ChallengeNotProvided)
                .leftWiden[ExtractionException]
                .map(_.toArray)
              _ <- EitherT.liftF(Async[F].cede)
              localSignature <- EitherT
                .liftF(ed25519Resource.use(e => Async[F].delay(e.sign(localPeerSK, remoteChallenge))))
              _ <- EitherT.liftF(Async[F].cede)
              _ <- EitherT.liftF(socket.write(Chunk.array(localSignature)))
              remoteSignature <- OptionT(socket.read(64))
                .toRight(ExtractionException.SignatureNotProvided)
                .leftWiden[ExtractionException]
                .map(_.toArray)
              _ <- EitherT.liftF(Async[F].cede)
              remoteSignatureIsValid <- EitherT.liftF(
                ed25519Resource
                  .use(e => Async[F].delay(e.verify(remoteSignature, localChallenge, Ed25519.PublicKey(remoteVK))))
              )
              _ <- EitherT.liftF(Async[F].cede)
              _ <- EitherT
                .cond[F](remoteSignatureIsValid, (), ExtractionException.InvalidSignature)
                .leftWiden[ExtractionException]
              isNewerPeer = java.util.Arrays.equals(remoteChallenge.slice(0, 16), PostLegacyChallengePrefix)
            } yield (remoteVKBS, isNewerPeer)
          ).value
      )

  sealed abstract class ExtractionException extends Exception

  object ExtractionException {
    case object VKNotProvided extends ExtractionException
    case object SelfConnection extends ExtractionException
    case object ChallengeNotProvided extends ExtractionException
    case object SignatureNotProvided extends ExtractionException
    case object InvalidSignature extends ExtractionException
  }

  /**
   * When sending the "challenge" to the remote peer, the challenge should use these 16 bytes as a prefix.
   * This allows backwards-compatibility with legacy peers, but allows for somewhat of "version detection" for new
   * clients.
   */
  val PostLegacyChallengePrefix: Array[Byte] =
    Array.fill(16)(1: Byte)

}
