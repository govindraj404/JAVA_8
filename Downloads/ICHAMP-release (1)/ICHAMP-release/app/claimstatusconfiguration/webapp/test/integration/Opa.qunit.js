sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/claimstatusconfiguration/test/integration/pages/MainListReport' ,
        'cmh/claimstatusconfiguration/test/integration/pages/MainObjectPage',
        'cmh/claimstatusconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/claimstatusconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);