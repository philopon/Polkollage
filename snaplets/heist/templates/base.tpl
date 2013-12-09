<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta charset="utf-8">
    <title><siteTitle/> - <subtitle/></title>
    <link rel="shortcut icon" href="/favicon.ico" />
    <link rel="stylesheet" href="${bootstrap.css}" type="text/css" />
    <link rel="stylesheet" href="/css/global.css" type="text/css" />
    <script src="/js/angular-file-upload-shim.min.js" />
    <script src="${angular.js}" />
    <script src="/js/angular-file-upload.min.js" />

    <in-header/>
    <script src="/js/site.js" />
  </head>
  <body ng-app="app">
    <nav class="navbar navbar-default" role="navigation">
        <div class="navbar-header">
          <a class="navbar-brand" href="/"><siteTitle/></a>
        </div>
        <ul class="nav navbar-nav">
          <ignore><li><a href="/recent">Recent</a></li></ignore>
        </ul>
    </nav>

    <div class="container">
      <apply-content/>
    </div>

    <footer>
      <p>Created by <a href="https://twitter.com/philomel202">@philopon202</a></p>
      <p>Served by <a href="http://snapframework.com/">Snap Web Framework</a></p>
    </footer>
    
  </body>
</html>
