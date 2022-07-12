sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/complainttypeconfiguration/test/integration/pages/MainListReport' ,
        'cmh/complainttypeconfiguration/test/integration/pages/MainObjectPage',
        'cmh/complainttypeconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/complainttypeconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);