using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.purchaseOrganization.PurchaseOrganizations with {

    ID
    @UI.Hidden : true
    @Common    : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : purchaseOrganization,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    purchaseOrganization
    @Common.Label: '{i18n>PURCHASING_ORGANIZATION}';
    
    purchaseOrganizationName
    @Common.Label: '{i18n>NAME}';

    companyCodeID
    @UI.HiddenFilter;

}

annotate cmh.purchaseOrganization.PurchaseOrganizations with @(Capabilities.SearchRestrictions.Searchable : true);


annotate cmh.purchaseOrganization.PurchaseOrganization with
@(Common.ValueListMapping : {
    Label          : '{i18n>PURCHASING_ORGANIZATION}',
    CollectionPath : 'PurchaseOrganizations',
    Parameters     : [
    {
        $Type             : 'Common.ValueListParameterInOut',
        ValueListProperty : 'ID',
        LocalDataProperty :  purchasingOrganization_ID
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'purchaseOrganization',
        ![@UI.Importance] : #High
    },
    {
        $Type             : 'Common.ValueListParameterDisplayOnly',
        ValueListProperty : 'purchaseOrganizationName',
        ![@UI.Importance] : #High
    },
    ]
});
