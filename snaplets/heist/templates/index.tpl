<bind tag="subtitle">水玉コラージュメーカー</bind>
<bind tag="in-header">
  <link rel="stylesheet" href="/css/index.css" type="text/css" />
</bind>

<bind tag="shares">
  <div class="pull-right">
    <a href="https://twitter.com/share" class="twitter-share-button" data-text="Polkollage 水玉コラージュメーカー" data-lang="ja" data-hashtags="polkollage">ツイート</a>
    <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
  </div>
</bind>

<apply template="base">
  <div ng-controller="UploadCtrl">
    
    <div id="drop-area" ng-file-drop="onFileSelect($files)" ng-file-drag-over-class="file-drag-over">
      
      <div id="drop-overlay" ><p>Drop here</p></div>
      
      <div class="page-header">
        <h1><siteTitle/> <small>水玉コラージュメーカー</small></h1>
      </div>

      <div id="share" class="clearfix">
        <shares/>
      </div>
      
      <p class="lead">画像をドロップまたは選択してスタート</p>
      
      <input type="hidden" name="token" ng-init="token='${token}'" ng-model="token"/>

      <div class="form-group">
        <input type="file" ng-file-select="onFileSelect($files)">
        <p class="help-block">対応ファイル: 8MBまでのJpeg,Png,Gif</p>
      </div> <!-- .form-group -->
      
      <div class="progress progress-striped active">
        <div class="progress-bar"  role="progressbar" aria-valuenow="{{progress}}" aria-valuemin="0" aria-valuemax="100" style="width: {{progress}}%">
          <span class="sr-only">{{progress}}% Complete</span>
        </div>
      </div> <!-- .progress -->
      
    </div><!-- #drop-area -->
  </div><!-- .controller -->
  
</apply>
