sap.ui.require(
    [
        'sap/fe/test/JourneyRunner',
        'cmh/complaintrsnconfiguration/test/integration/pages/MainListReport' ,
        'cmh/complaintrsnconfiguration/test/integration/pages/MainObjectPage',
        'cmh/complaintrsnconfiguration/test/integration/OpaJourney'
    ],
    function(JourneyRunner, MainListReport, MainObjectPage, Journey) {
        'use strict';
        var JourneyRunner = new JourneyRunner({
            // start index.html in web folder
            launchUrl: sap.ui.require.toUrl('cmh/complaintrsnconfiguration') + '/index.html'
        });

        
        JourneyRunner.run(
            {
                pages: { onTheMainPage: MainListReport, onTheDetailPage: MainObjectPage }
            },
            Journey.run
        );
        
    }
);