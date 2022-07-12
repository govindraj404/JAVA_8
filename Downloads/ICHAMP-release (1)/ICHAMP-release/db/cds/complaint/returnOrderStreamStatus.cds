namespace com.sap.ic.cmh.returnOrderStreamStatus;

using {com.sap.ic.cmh.returnPurchaseOrderStatus.ReturnPurchaseOrderStatus} from '../returnPurchaseOrder/index';
using {com.sap.ic.cmh.streamStatus.StreamStatus } from './index';

type LogisticsStreamStatus : Association to one LogisticsStreamStatuses;

entity LogisticsStreamStatuses {
    key returnPurchaseOrderStatus : ReturnPurchaseOrderStatus;
    key streamStatus : StreamStatus;
}