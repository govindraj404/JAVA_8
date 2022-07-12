sap.ui.define(['sap/ui/test/opaQunit',
'sap/ui/test/actions/Press',
'sap/ui/test/Opa5',
'sap/ui/test/matchers/Properties'
], function(opaTest,Press,Opa5,Properties) {
    'use strict';

    var Journey = {
        run: function() {
            QUnit.module('Sample journey');

            opaTest('#000: Start', function(Given, When, Then) {
                Given.iResetTestData().and.iStartMyApp();
                Then.onTheMainPage.iSeeThisPage();
            });

            opaTest('#1: ListReport: Check List Report Page loads', function(Given, When, Then) {
                When.onTheMainPage.onFilterBar().iExecuteSearch();
                Then.onTheMainPage.onTable().iCheckRows();
            });

            opaTest('#2: Object Page: Check Object Page loads', function(Given, When, Then) {
                Then.onTheMainPage.iSeeThisPage();
            });

            opaTest('#3 List Report : Executes the Create action on the table.',function (Given,When,Then){
                When.onTheMainPage.onTable().iExecuteCreate();
            });

            opaTest('#4 List Report : Check Selecting the row',function (Given,When,Then){
                When.onTheMainPage.onTable().iPressRow();
            });

            opaTest('#5 Object Page : Check Edit button',function (Given,When,Then){
                When.onTheDetailPage.onHeader().iExecuteEdit();
            });

            opaTest('#6: Object Page : Check Adapt streams Button',function(Given,When,Then){
                When.waitFor({
                    id : "cmh.managecomplaint::ComplaintsObjectPage--fe::table::businessObjects::LineItem::CustomAction::edit",
                    actions : new Press(),
                    errorMessage : "Did not find the Adapt Streams button"
                });

                Then.waitFor({
                    controlType : "sap.m.Dialog",
                    success : function () {
                        Opa5.assert.ok(true, "Adapt Streams Dialog is Opened");
                    },
                    errorMessage: "Did not find the dialog control"
                });

                When.waitFor({
                    controlType : "sap.m.Button",
                    searchOpenDialogs: true,
                    id: "cmh.managecomplaint::ComplaintsObjectPage--sEditStreamCloseButton",
                    actions: new Press(),
                    errorMessage : "Did not find the Close button"                    
                });

                Then.waitFor({
                    controlType : "sap.m.Button",
                    success : function () {
                        Opa5.assert.ok(true, "Adapt Streams Dialog is Closed");
                    },
                    errorMessage: "Did not find the Close Button control"
                });
            });

            opaTest('#7:Object Page : Check Create Claim Button',function(Given,When,Then){
                When.waitFor({
                    controlType: "sap.m.Button",
                    viewId: "cmh.managecomplaint::ComplaintsObjectPage",
                    enabled: false,
			        matchers: new Properties({
				        text: "Create Claim"
                    }),
                    success: function (aButtons) {
				     Opa5.assert.ok(true, "Found the Create Claim button: " + aButtons[0]);
                    }, 
                    actions : new Press(),
                    errorMessage: "Did not find the disabled button" 
                }); 
                         
			}); 
            
            opaTest('#999: Tear down', function(Given, When, Then) {
                Given.iTearDownMyApp();
            });

                        
        }
    };

    return Journey;
});