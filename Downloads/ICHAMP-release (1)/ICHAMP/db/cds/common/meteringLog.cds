namespace com.sap.ic.cmh.meteringLog;

using {cuid,managed} from '@sap/cds/common';

entity MeteringLogs : cuid {
 	time : Timestamp;
    status	  : String;
    actionExecutionCount : Integer;
}