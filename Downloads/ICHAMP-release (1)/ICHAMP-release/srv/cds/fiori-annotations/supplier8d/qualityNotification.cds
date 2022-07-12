using SupplierIssueProcessService from '../../services/index';

annotate SupplierIssueProcessService.QualityNotifications with {
        identifier
        @Common.Label        : '{i18n>QUALITY_NOTIFICATION_NUMBER}';

        purchaseOrderNumber
        @Search.defaultSearchElement
        @Common.Label        : '{i18n>PURCHASE_ORDER_NUMBER}'
        @Common.FieldControl : #ReadOnly; 

        purchaseOrderItem
        @Search.defaultSearchElement
        @Common.Label        : '{i18n>PURCHASE_ORDER_ITEM}'
        @Common.FieldControl : #ReadOnly;

}