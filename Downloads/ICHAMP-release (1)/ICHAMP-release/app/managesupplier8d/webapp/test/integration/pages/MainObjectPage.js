sap.ui.define(['sap/fe/test/ObjectPage'], function(ObjectPage) {
    'use strict';

    // OPTIONAL
    var AdditionalCustomObjectPageDefinition = {
        actions: {},
        assertions: {}
    };

    return new ObjectPage(
        {
            appId: 'cmh.managesupplier8d', // MANDATORY: Compare sap.app.id in manifest.json
            componentId: 'Supplier8DProcessesObjectPage', // MANDATORY: Compare sap.ui5.routing.targets.id in manifest.json
            entitySet: 'Supplier8DProcesses' // MANDATORY: Compare entityset in manifest.json
        },
        AdditionalCustomObjectPageDefinition
    );
});