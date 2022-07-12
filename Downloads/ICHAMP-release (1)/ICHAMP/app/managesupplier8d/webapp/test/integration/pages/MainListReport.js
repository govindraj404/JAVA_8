sap.ui.define(['sap/fe/test/ListReport'], function(ListReport) {
    'use strict';

    var AdditionalCustomListReportDefinition = {
        actions: {},
        assertions: {}
    };

    return new ListReport(
        {
            appId: 'cmh.managesupplier8d',
            componentId: 'Supplier8DProcessesList',
            entitySet: 'Supplier8DProcesses'
        },
        AdditionalCustomListReportDefinition
    );
});