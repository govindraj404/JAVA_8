sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/manageclaim/test/integration/pages/MainListReport' ,
        'cmh/manageclaim/test/integration/pages/MainObjectPage',
        'cmh/manageclaim/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/manageclaim') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);