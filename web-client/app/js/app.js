'use strict';

/* App Module */

var phonecatApp = angular.module('phonecatApp', [
  'ngRoute',
  'phonecatControllers'
]);

phonecatApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/solarbodies', {
        templateUrl: 'partials/solarbodies.html',
        controller: 'SolarBodiesCtrl'
      }).
      when('/stations/:solarBodyId/:solarBodyName', {
        templateUrl: 'partials/stations.html',
        controller: 'StationsCtrl'
      }).
      when('/sensors/:stationId/:stationName', {
        templateUrl: 'partials/sensors.html',
        controller: 'SensorsCtrl'
      }).
      otherwise({
        redirectTo: '/solarbodies'
      });
  }]);
