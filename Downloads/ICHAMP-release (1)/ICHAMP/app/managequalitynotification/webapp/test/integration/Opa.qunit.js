sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/managequalitynotification/test/integration/pages/MainListReport' ,
        'cmh/managequalitynotification/test/integration/pages/MainObjectPage',
        'cmh/managequalitynotification/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/managequalitynotification') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);