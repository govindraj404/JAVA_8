sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/complaintrsnmapconfiguration/test/integration/pages/MainListReport' ,
        'cmh/complaintrsnmapconfiguration/test/integration/pages/MainObjectPage',
        'cmh/complaintrsnmapconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/complaintrsnmapconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);