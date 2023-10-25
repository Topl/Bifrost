import React from 'react';
import Link from '@docusaurus/Link';
import {useLocation} from '@docusaurus/router';
import {usePluginData} from '@docusaurus/useGlobalData';

// Only the 'docs' plugin pages are versioned.
function getVersionPath() {
  const {pathname} = useLocation();
  const {versions} = usePluginData('docusaurus-plugin-content-docs');
  // Default to 'current' if no version is specified.
  return versions.find(({path}) => pathname.startsWith(path)).name || "current";
}

export default function ScaladocLink({children, path}) {
  var version = getVersionPath()
  return (
    <Link to={`/scaladoc/${version}/${path}`} target="_blank">{children}</Link>
  );
}
