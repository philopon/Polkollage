angular.module('app', ['angularFileUpload'])

.controller('UploadCtrl', function($scope, $upload, $window){
  $scope.progress = 0;
  $scope.onFileSelect = function($files, $token){
    $scope.upload = $upload.upload({
      url: "/raw",
      method: "POST",
      params: {token: $scope.token},
      file: $files[0],
      fileFormDataName: "image"
    }).progress(function(evt){
      $scope.progress = 100 * evt.loaded / evt.total;
    }).success(function(data,status,headers,config){
      $window.location.href = "/edit/" + data.id;
    }).error(function(data,status,headers,config){
      $window.alert("アップロードに失敗しました。: " + data);
    });
  }
})

.controller('EditCtrl', function($scope, $http, $window){

  var init_color = 'rgba(0, 0, 255, 0.5)';

  $scope.inQuery  = false;
  $scope.useAlpha = false;
  $scope.deletePassword = "";

  $scope.showPreview = function(){
    var param = $scope.createParam("GET", 'arraybuffer');
    if(!param){$window.alert("1つ以上の水玉を描いてください。"); return -1}

    $scope.inQuery = true;
    $http(param).success(function(data, status, headers, config){
      var u8 = new Uint8Array(data);
      binary = "";
      for (var i = 0; i < u8.byteLength; i++) {
        binary += String.fromCharCode(u8[i]);
      }
      var b64 = "data:image/png;base64," + btoa(binary);
      $scope.editor.showPreview(b64);
      $scope.inQuery = false;
    }).error(function(data,status,headers,config){
      $window.alert("プレビューの取得に失敗しました。" + data);
      $scope.inQuery = false;
    });
  }

  $scope.saveImage = function(){
    var param = $scope.createParam("POST");
    if(!param){$window.alert("1つ以上の水玉を描いてください。"); return -1}
    param['params']['deletePassword'] = $scope.deletePassword;

    $scope.inQuery = true;
    $http(param).success(function(data, status, headers, config){
      $window.location.href = "/image/" + data.id;
      $scope.inQuery = false;
    }).error(function(data,status,headers,config){
      $window.alert("画像の作成に失敗しました。" + data);
      $scope.inQuery = false;
    }); 
  }

  $scope.initialize = function(ident){
    $scope.ident          = ident;
    $scope.editor         = new Editor ("edit", ident, init_color);
    $scope.picker         = $('#colorpicker');
    
    $scope.editor.onShowPreview = function(){$scope.$apply('previewShown = true');}
    $scope.editor.onEndPreview  = function(){
      if($scope.$root.$$phase){ $scope.previewShown = false }
      else {$scope.$apply('previewShown = false');}
    }
    
    $('body').keypress(function(e){
      if (e.charCode == 100) {
        $scope.editor.removeSelected();
      }
    });
    
    $scope.picker.on('changeColor', function(ev){
      var c = ev.color.toRGB();
      $scope.color = [c.r / 255.0, c.g / 255.0, c.b / 255.0, parseFloat(c.a)];
    }).on('hide', function(ev){
      var c = ev.color.toRGB();
      var str = "rgba(" + c.r + "," + c.g + "," + c.b + "," + c.a + ")"
      $scope.editor.setColor(str);
      if($scope.previewShown){
        $scope.showPreview();
      }
    });

    $scope.picker.colorpicker('setValue', init_color);
    $('#submitButton').tooltip({container: 'body',
                                placement: 'right',
                                title: function(){if($scope.previewShown){return "保存"}
                                                  else{return "プレビュー"}}
                                });
    $('#removeButton').tooltip({container: 'body',
                                placement: 'right',
                                title: function(){if($scope.previewShown){return "プレビュー終了"}
                                                  else{return "削除(d)"}}
                                });

    $('#configButton').tooltip({container: 'body',
                                placement: 'right',
                                title:     '設定'
                                }).modal({show: false});
    
  } // end initialize

  $scope.createParam = function(method, responseType){
    var circles = $scope.editor.getData();
    var color   = $scope.color;
    if(!$scope.useAlpha){
      color   = [$scope.color[0], $scope.color[1], $scope.color[2], 1]
    }
    
    if(circles.length < 1){return null}
    var query = {ident: $scope.ident,
                 color: JSON.stringify(color),
                 data: JSON.stringify(circles)};
    var param = {url:    '/edit',
                 method: method,
                 params: query,
                 responseType: responseType
                 };
    return param
  }

  $scope.remove = function(){
    $('#removeButton').tooltip('hide')
    if($scope.previewShown){
      $scope.editor.endPreview();
    } else {
      $scope.editor.removeSelected();
    }
  }

  $scope.submit = function(){
    $('#submitButton').tooltip('hide')
    if($scope.previewShown){
      $('#submitModal').modal('show');
    } else {
      $scope.showPreview();
    }
  }

  

  $scope.showPicker = function(){
      $('#colorpicker').colorpicker('show');
  }
})

.controller('ImageCtrl', function($scope, $window, $http){
  $scope.original = false;
  $scope.deletePassword = "";
  $scope.deleteAlertMessage = null;
  $scope.deleteAlertClass   = null;

  $scope.showDeleteModal = function(){$('#deleteModal').modal('show')
        .on('hidden.bs.modal', function (e) {$scope.deleteAlertMessage = null;
                                             $scope.deleteAlertClass   = null;})}
  
  $scope.deleteImage = function(){
    var param = {url:    '/image',
                 method: 'DELETE',
                 params: {'id': $scope.ident, 'deletePassword': $scope.deletePassword}
                 };
    $http(param).success(function(){
      $scope.deleteAlertClass   = "alert-success";
      $scope.deleteAlertMessage = "削除しました。";
      $('#deleteModal').on('hidden.bs.modal', function(){
        $window.location.href = "/";
      })
    }).error(function(data,status,headers,config){
      $scope.deleteAlertClass   = "alert-danger";
      if(status == 401){$scope.deleteAlertMessage = "パスワードが違います。";}
      else             {$scope.deleteAlertMessage = data}
    })
  }
});
