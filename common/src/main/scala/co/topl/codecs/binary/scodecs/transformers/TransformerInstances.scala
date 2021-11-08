package co.topl.codecs.binary.scodecs.transformers

import scodec.{Transform, Transformer}

import scala.collection.SortedSet
import scala.reflect.ClassTag

trait TransformerInstances {

  implicit def listToSetTransformer[T]: Transformer[List[T], Set[T]] =
    new Transformer[List[T], Set[T]] {

      override def apply[F[_]: Transform](fa: F[List[T]]): F[Set[T]] =
        Transform[F].xmap[List[T], Set[T]](fa, list => list.toSet, set => set.toList)
    }

  implicit def listToSortedSetTransformer[T: Ordering]: Transformer[List[T], SortedSet[T]] =
    new Transformer[List[T], SortedSet[T]] {

      override def apply[F[_]: Transform](fa: F[List[T]]): F[SortedSet[T]] =
        Transform[F].xmap[List[T], SortedSet[T]](fa, listT => SortedSet[T](listT: _*), sortedSet => sortedSet.toList)
    }

  implicit def listToIndexedSeqTransformer[T]: Transformer[List[T], IndexedSeq[T]] =
    new Transformer[List[T], IndexedSeq[T]] {

      override def apply[F[_]: Transform](fa: F[List[T]]): F[IndexedSeq[T]] =
        Transform[F].xmap[List[T], IndexedSeq[T]](fa, listT => listT.toIndexedSeq, seqT => seqT.toList)
    }

  implicit def listToSeqTransformer[T]: Transformer[List[T], Seq[T]] =
    new Transformer[List[T], Seq[T]] {

      override def apply[F[_]: Transform](fa: F[List[T]]): F[Seq[T]] =
        Transform[F].xmap[List[T], Seq[T]](fa, listT => listT.toSeq, seqT => seqT.toList)
    }

  implicit def listToArrayTransformer[T: ClassTag]: Transformer[List[T], Array[T]] =
    new Transformer[List[T], Array[T]] {

      override def apply[F[_]: Transform](fa: F[List[T]]): F[Array[T]] =
        Transform[F].xmap[List[T], Array[T]](fa, listT => listT.toArray, arrayT => arrayT.toList)
    }
}
