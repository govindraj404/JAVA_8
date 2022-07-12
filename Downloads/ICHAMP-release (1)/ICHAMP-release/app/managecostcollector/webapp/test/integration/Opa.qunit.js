sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/managecostcollector/test/integration/pages/MainListReport' ,
        'cmh/managecostcollector/test/integration/pages/MainObjectPage',
        'cmh/managecostcollector/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/managecostcollector') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);