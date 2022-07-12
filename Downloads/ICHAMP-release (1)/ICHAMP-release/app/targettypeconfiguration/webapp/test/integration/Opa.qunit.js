sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/targettypeconfiguration/test/integration/pages/MainListReport' ,
        'cmh/targettypeconfiguration/test/integration/pages/MainObjectPage',
        'cmh/targettypeconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/targettypeconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);