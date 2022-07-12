sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/targetreftypemapconfiguration/test/integration/pages/MainListReport' ,
        'cmh/targetreftypemapconfiguration/test/integration/pages/MainObjectPage',
        'cmh/targetreftypemapconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/targetreftypemapconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);