sap.ui.define(['sap/ui/test/opaQunit','sap/ui/test/actions/Press','sap/ui/test/Opa5'], function(opaTest,Press,Opa5) {
    'use strict';

    var Journey = {
        run: function() {
            QUnit.module('Sample journey');

            opaTest('#000: Start', function(Given, When, Then) {
                Given.iResetTestData().and.iStartMyApp();
                Then.onTheMainPage.iSeeThisPage();
            });

            opaTest('#1: Object Page : Check Object Details Button',function(Given,When,Then){
                When.waitFor({
                    id : "cmh.manageclaim::ClaimsObjectPage--fe::CustomAction::NavigationAction",
                    actions : new Press(),
                    errorMessage : "Did not find the Object Details button"
                });

                Then.waitFor({
                    controlType : "sap.m.Button",
                    success : function () {
                        Opa5.assert.ok(true, "Navigation Successfull");
                    },
                    errorMessage: "Navigation failed"
                });
             });
            
            opaTest('#999: Tear down', function(Given, When, Then) {
                Given.iTearDownMyApp();
            });
        }
    };

    return Journey;
});
