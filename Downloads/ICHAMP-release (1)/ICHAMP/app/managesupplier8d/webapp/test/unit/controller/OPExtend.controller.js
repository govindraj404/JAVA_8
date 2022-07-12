sap.ui.define(["cmh/managesupplier8d/ext/controller/OPExtend.controller",
"sap/ui/model/json/JSONModel",
"sap/ui/thirdparty/sinon",
"sap/ui/thirdparty/sinon-qunit"
], function (ObjectPageExtController, JSONModel) {
	"use strict";	

	QUnit.module("Object Page Extend Controller", function (hooks) {
		
        hooks.beforeEach(function () {
			this._oApplicationModel = new JSONModel();
			this._oAppController = new ObjectPageExtController();
		});

		hooks.afterEach(function () {
			this._oApplicationModel.destroy();
			this._oAppController.destroy();
		});         

		QUnit.test("Should see that the onPageReady is called or not", function(assert){
				var bCalled = false;		
				
			this._oAppController.getMetadata().getOverrides().base = {
				
				getAppComponent: function() {
					return {
						getShellServices: function() {
                            // bCalled = true;
							return {
								shellNavigation: {
                                    hashChanger: {
                                        _oNavigationState: {
                                            newHash: "IsActiveEntity=false"
                                        }
                                    }
                                }
							};
						}
					};
                },
                byId: function() {	
					return {				
						setValue: function() {
							bCalled = true;
						}
						
					};
				},
            };
            
            this._oAppController.getMetadata().getOverrides().getView = function() {				
					return {
						getBindingContext: function() {
							return {
								getObject: function() {
                                    bCalled = true;
									return {
										HasActiveEntity : false
									};								
								}
							};						
						},
						byId: function() {
							return {
								getSelectedButton: function() {
									bCalled = true;
								},
								setVisible: function() {
									bCalled = true;
								},
								setSelectionMode: function() {
									bCalled = true;
								},
								clearSelection: function() {
									bCalled = true;
								},
								attachSelectionChange: function() {
									bCalled = true;
								}
							};							
						},
						getModel: function() {
							return {
								setProperty: function() {
									bCalled = true;
								}
							};
						}
					};				
			}.bind(this);	
				
			this._oAppController.getMetadata().getOverrides().onPageReady();
			assert.ok(bCalled, "onPageReady method called successfully");
		});		
    });

});