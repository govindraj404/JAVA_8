sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/managecomplaint/test/integration/pages/MainListReport' ,
        'cmh/managecomplaint/test/integration/pages/MainObjectPage',
        'cmh/managecomplaint/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/managecomplaint') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);