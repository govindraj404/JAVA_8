sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/customercomplaint/test/integration/pages/MainListReport' ,
        'cmh/customercomplaint/test/integration/pages/MainObjectPage',
        'cmh/customercomplaint/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/customercomplaint') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);