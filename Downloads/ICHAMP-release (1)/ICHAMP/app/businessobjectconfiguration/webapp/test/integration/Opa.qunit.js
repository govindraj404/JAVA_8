sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/businessobjectconfiguration/test/integration/pages/MainListReport' ,
        'cmh/businessobjectconfiguration/test/integration/pages/MainObjectPage',
        'cmh/businessobjectconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/businessobjectconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);