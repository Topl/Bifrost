import React from 'react';
import clsx from 'clsx';
import styles from './styles.module.css';

const FeatureList = [
  {
    title: 'Easy to Use',
    Svg: require('@site/static/img/dev-friendly.svg').default,
    description: (
      <>
        Run the blockchain node on your PC, a server, or any cloud enviornment.
      </>
    ),
  },
  {
    title: 'Future proof',
    Svg: require('@site/static/img/future-proof.svg').default,
    description: (
      <>
        Topl’s digital asset model can support fungible, non-fungible, and semi-fungible tokens, all defined without smart contracts, unlocking novel uses for developers and future-proofing against new use cases.
      </>
    ),
  },
    {
    title: 'Fully Bitcoin compliant',
    Svg: require('@site/static/img/bitcoin-compliant.svg').default,
    description: (
      <>
        No more walled gardens – Topl is built to connect the Bitcoin economy across ecosystems, making it the future-proof home for your next project.
      </>
    ),
  }
];

function Feature({Svg, title, description}) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={styles.featureSvg} role="img" />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
