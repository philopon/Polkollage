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

  var init_color = 'rgba(0, 0, 255, 0.8)';

  $scope.showPreview = function(){
    $http($scope.createParam("GET", 'arraybuffer')).success(function(data, status, headers, config){
      var b64 = "data:image/png;base64," + btoa(String.fromCharCode.apply(null, new Uint8Array(data)));
      $scope.editor.showPreview(b64);
    }).error(function(data,status,headers,config){
      $window.alert("プレビューの取得に失敗しました。" + data);
    });
  }

  $scope.saveImage = function(){
    $http($scope.createParam("POST")).success(function(data, status, headers, config){
      $window.location.href = "/image/" + data.id;
    }).error(function(data,status,headers,config){
      $window.alert("画像の作成に失敗しました。" + data);
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
    
  } // end initialize

  $scope.createParam = function(method, responseType){
    var query = {ident: $scope.ident,
                 color: JSON.stringify($scope.color),
                 data: JSON.stringify($scope.editor.getData())};
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
      $scope.saveImage();
    } else {
      $scope.showPreview();
    }
  }

  $scope.showPicker = function(){
      $('#colorpicker').colorpicker('show');
  }
})

.controller('ImageCtrl', function($scope){
  $scope.original = false;
});
