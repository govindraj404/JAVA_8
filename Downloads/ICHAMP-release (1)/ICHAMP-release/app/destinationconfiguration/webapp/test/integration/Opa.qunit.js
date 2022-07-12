sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/destinationconfiguration/test/integration/pages/MainListReport' ,
        'cmh/destinationconfiguration/test/integration/pages/MainObjectPage',
        'cmh/destinationconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/destinationconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);