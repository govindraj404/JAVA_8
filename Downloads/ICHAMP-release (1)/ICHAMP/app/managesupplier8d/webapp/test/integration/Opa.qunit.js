sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/managesupplier8d/test/integration/pages/MainListReport' ,
        'cmh/managesupplier8d/test/integration/pages/MainObjectPage',
        'cmh/managesupplier8d/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/managesupplier8d') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);