namespace com.sap.ic.cmh.refundControl;

using {com.sap.ic.cmh.configuration.dataType as DataType} from './index';

type RefundControl : Association to one RefundControls;

entity RefundControls {
    key  code        : DataType.RefundControlCode default 'R';
        description : localized DataType.RefundControlDescription;
}
