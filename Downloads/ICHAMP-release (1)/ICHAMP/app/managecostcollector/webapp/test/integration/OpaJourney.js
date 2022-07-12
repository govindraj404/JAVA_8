sap.ui.define(['sap/ui/test/opaQunit','sap/ui/test/actions/Press','sap/ui/test/Opa5'], function(opaTest,Press,Opa5) {
    'use strict';

    var Journey = {
        run: function() {
            QUnit.module('Sample journey');

            opaTest('#000: Start', function(Given, When, Then) {
                Given.iResetTestData().and.iStartMyApp();
                Then.onTheMainPage.iSeeThisPage();
            });

            opaTest('#1: Object Page: Check Object Page loads', function(Given, When, Then) {
                Then.onTheMainPage.iSeeThisPage();
            });
         
            opaTest('#2: Cost Collector Object Page : Check Add Button',function(Given,When,Then){
                When.waitFor({
                    id : "cmh.managecostcollector::ComplaintsObjectPage--fe::table::costCollector::LineItem::CustomAction::MenuActions-internalBtn",
                    actions : new Press(),
                    errorMessage : "Did not find the Add button"
                });

                Then.waitFor({
                    controlType : "sap.m.Dialog",
                    success : function () {
                        Opa5.assert.ok(true, "Add Dialog is Opened");
                    },
                    errorMessage: "Did not find the dialog control"
                });

            });
               
            opaTest('#999: Tear down', function(Given, When, Then) {
                Given.iTearDownMyApp();
            });
        }
    };

    return Journey;
});
