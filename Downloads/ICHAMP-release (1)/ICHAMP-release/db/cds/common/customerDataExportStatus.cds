namespace com.sap.ic.cmh.customerDataExportStatus;

using {cuid,managed} from '@sap/cds/common';

type CustomerDataExportStatus : Association to one CustomerDataExportStatuses;

entity CustomerDataExportStatuses : cuid, managed {
        status   : String (20);
        error   : String (20);
        tenantId   : String (40);
        fileName : String (100);
}