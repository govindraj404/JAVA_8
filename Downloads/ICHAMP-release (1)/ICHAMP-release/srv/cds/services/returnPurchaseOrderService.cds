using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';
using {com.sap.ic.cmh.returnPurchaseOrder.dataType as datatype} from '../../../db/cds/returnPurchaseOrder/index';

service ReturnPurchaseOrderService {
    entity ReturnPurchaseOrders as projection on cmh.returnPurchaseOrder.ReturnPurchaseOrders{
            *,
      @Core.Computed: false 7 as isReturnOrderFieldControlMandatory : datatype.FieldControl,
      @Core.Computed: false 3 as isReturnOrderFieldControl : datatype.FieldControl,
      @Core.Computed: false false as isUpdateRestricted : datatype.UpdateRestricted,
      @Core.Computed: false '' as sNavigation         : String(20),
      '' as number : datatype.Identifier
    };
    entity LogisticsStreamStatuses as projection on cmh.returnOrderStreamStatus.LogisticsStreamStatuses;
}
