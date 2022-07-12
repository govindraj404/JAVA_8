sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/sourcereftypemapconfiguration/test/integration/pages/MainListReport' ,
        'cmh/sourcereftypemapconfiguration/test/integration/pages/MainObjectPage',
        'cmh/sourcereftypemapconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/sourcereftypemapconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);