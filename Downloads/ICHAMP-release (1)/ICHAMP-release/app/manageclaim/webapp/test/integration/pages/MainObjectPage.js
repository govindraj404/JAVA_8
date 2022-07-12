sap.ui.define(['sap/fe/test/ObjectPage'], function(ObjectPage) {
    'use strict';

    // OPTIONAL
    var AdditionalCustomObjectPageDefinition = {
        actions: {},
        assertions: {}
    };

    return new ObjectPage(
        {
            appId: 'cmh.manageclaim', // MANDATORY: Compare sap.app.id in manifest.json
            componentId: 'ClaimsObjectPage', // MANDATORY: Compare sap.ui5.routing.targets.id in manifest.json
            entitySet: 'Claims' // MANDATORY: Compare entityset in manifest.json
        },
        AdditionalCustomObjectPageDefinition
    );
});