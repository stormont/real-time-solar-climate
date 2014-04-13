'use strict';

/* Controllers */

var phonecatControllers = angular.module('phonecatControllers', []);

phonecatControllers.controller('SolarBodiesCtrl', ['$scope', '$http',
  function($scope, $http) {
    $http.get('http://localhost:8000/v0.1/get/solarbodies').success(function(data) {
      $scope.solarbodies = data;
    });
  }]);

phonecatControllers.controller('StationsCtrl', ['$scope', '$http', '$routeParams',
  function($scope, $http, $routeParams) {
    $http.get('http://localhost:8000/v0.1/get/stations?solarbodyid=' + $routeParams.solarBodyId).success(function(data) {
      $scope.stations = data;
      $scope.solarBodyName = $routeParams.solarBodyName;
    });
  }]);

phonecatControllers.controller('SensorsCtrl', ['$scope', '$http', '$routeParams',
  function($scope, $http, $routeParams) {
    $http.get('http://localhost:8000/v0.1/get/sensors?stationid=' + $routeParams.stationId).success(function(data) {
      $scope.sensors = data;
      $scope.stationName = $routeParams.stationName;

      $http.get('http://localhost:8000/v0.1/get/feedtypes').success(function(feedTypeData) {
        $scope.feedTypes = feedTypeData;

        for (var k = 0; k < $scope.sensors.length; k++) {
          $scope.sensors[k].frequency.lastUpdate = new Date($scope.sensors[k].frequency.lastUpdate * 1000);

          for (var i = 0; i < $scope.sensors[k].levels.length; i++) {
            for (var j = 0; j < $scope.feedTypes.length; j++) {
              if ($scope.feedTypes[j].id == $scope.sensors[k].levels[i].feedTypeID) {
                $scope.sensors[k].levels[i].typeName = $scope.feedTypes[j].type;
                $scope.sensors[k].levels[i].units = $scope.feedTypes[j].units;
                break;
              }
            }
          }
        }
      });
    });
  }]);
