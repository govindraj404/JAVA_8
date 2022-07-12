sap.ui.define(['sap/ui/test/opaQunit','sap/ui/test/actions/Press','sap/ui/test/Opa5','sap/ui/test/matchers/Properties'], function(opaTest,Press,Opa5,Properties) {
    'use strict';

    var Journey = {
        run: function() {
            QUnit.module('Sample journey');

            opaTest('#000: Start', function(Given, When, Then) {
                Given.iResetTestData().and.iStartMyApp();
                Then.onTheMainPage.iSeeThisPage();
            });

            opaTest('#1: ListReport: Check List Report Page loads', function(Given, When, Then) {
                Then.onTheMainPage.iSeeThisPage();
            });

            opaTest('#2: Object Page: Check Object Page loads', function(Given, When, Then) {
                Then.onTheMainPage.iSeeThisPage();
            });

            
            opaTest('#999: Tear down', function(Given, When, Then) {
                Given.iTearDownMyApp();
            });
        }
    };

    return Journey;
});
