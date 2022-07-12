using ReturnPurchaseOrderService from '../../services/index';

annotate ReturnPurchaseOrderService.ReturnPurchaseOrders with @(
    UI : {
        LineItem : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            }
        ]
    }
); 