<bind tag="subtitle">Image - <ident/></bind>
<bind tag="in-header">
  <link rel="stylesheet" href="/css/image.css" type="text/css" />
  <script src="${jquery.js}"/>
  <script src="${bootstrap.js}"/>
</bind>

<bind tag="shares">

    <div class="pull-left">
      <a class="btn btn-default btn-xs" href="/edit/${original}">
        <span class="glyphicon glyphicon-pencil"></span>この画像でコラージュを作る
      </a>
    </div>
    
    <div class="pull-left">
      <a href="https://twitter.com/share" class="twitter-share-button" data-text="Polkollage 水玉コラージュメーカー" data-lang="ja" data-hashtags="polkollage">ツイート</a>
      <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
    </div>

    <div class="pull-right">
      <button class="btn btn-default btn-xs" ng-click="showDeleteModal()">
        <span class="glyphicon glyphicon-pencil"></span>削除
      </button>
  </div>

</bind>

<apply template="base">
  
  <div ng-controller="ImageCtrl" ng-init="ident=${ident}">

    <!-- Delete Modal -->
    <div class="modal fade" id="deleteModal" tabindex="-1" role="dialog" aria-labelledby="deleteModalLabel" aria-hidden="true">
      <div class="modal-dialog">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>
            <h4 class="modal-title" id="deleteModalLabel">削除</h4>
          </div>
          <div class="modal-body">

            <form class="form-horizontal" role="form" ng-submit="deleteImage()">

            <div class="form-group">
              <label for="deletePassword" class="col-sm-4 control-label">削除パスワード</label>
              <div class="col-sm-4">
                <input type="password" class="form-control" id="deletePassword" ng-model="deletePassword" placeholder="1文字以上の文字列">
              </div>
            </div>

            </form>

            <div ng-show="deleteAlertMessage" class="alert {{deleteAlertClass}}">{{deleteAlertMessage}}</div>

            <div class="modal-footer">
              <button type="button" class="btn btn-default" data-dismiss="modal">キャンセル</button>
              <button type="button" class="btn btn-primary" ng-disabled="deletePassword.length < 1" ng-click="deleteImage()">削除</button>
            </div>

          </div>
        </div><!-- /.modal-content -->
      </div><!-- /.modal-dialog -->
    </div><!-- /.modal -->




    
    <div style="width: ${width}px" class="row">
      <shares/>
    </div>

    <div class="row">
      <img title="クリックすると元画像と切り替わります。" width="${width}" height="${height}" ng-click="original=true"  ng-hide="original" src="/image/png/${ident}" />
      <img title="クリックすると元画像と切り替わります。" width="${width}" height="${height}" ng-click="original=false" ng-show="original" src="/raw/${original}" />
    </div>

  </div>  
</apply>
