sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/managereturnpurchaseorder/test/integration/pages/MainListReport' ,
        'cmh/managereturnpurchaseorder/test/integration/pages/MainObjectPage',
        'cmh/managereturnpurchaseorder/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/managereturnpurchaseorder') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);