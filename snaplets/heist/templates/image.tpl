<bind tag="subtitle">Image - <ident/></bind>
<bind tag="in-header">
  <link rel="stylesheet" href="/css/image.css" type="text/css" />
</bind>

<bind tag="shares">
  <div class="clearfix">

    <div class="pull-left">
      <a class="btn btn-default btn-xs" href="/edit/${original}">
        <span class="glyphicon glyphicon-pencil"></span>この画像でコラージュを作る
      </a>
    </div>
    
    <div class="pull-left">
      <a href="https://twitter.com/share" class="twitter-share-button" data-text="Polkollage 水玉コラージュメーカー" data-lang="ja" data-hashtags="polkollage">ツイート</a>
      <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
    </div>

  </div>
</bind>

<apply template="base">
  
  <div ng-controller="ImageCtrl">
    <div class="row">
      <shares/>
    </div>

    <div class="row">
      <img title="クリックすると元画像と切り替わります。" width="${width}" height="${height}" ng-click="original=true"  ng-hide="original" src="/image/png/${ident}" />
      <img title="クリックすると元画像と切り替わります。" width="${width}" height="${height}" ng-click="original=false" ng-show="original" src="/raw/${original}" />
    </div>

  </div>  
</apply>
