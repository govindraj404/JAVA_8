using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.salesOrganization.SalesOrganizations with {

    ID
    @UI.Hidden    : true
    @Common       : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : salesOrganization,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    salesOrganization
    @Common.Label : '{i18n>SALES_ORGANIZATION}';

    salesOrganizationName
    @Common.Label : '{i18n>DESCRIPTION}';

    companyCodeID
    @UI.HiddenFilter;

    businessPartnerID_ID
    @UI.HiddenFilter;

    currency
    @UI.HiddenFilter;
}

annotate cmh.salesOrganization.SalesOrganizations with @(Capabilities.SearchRestrictions.Searchable : true);


annotate cmh.salesOrganization.SalesOrganization with
@(Common.ValueListMapping : {
    Label          : '{i18n>SALES_ORGANIZATION}',
    CollectionPath : 'SalesOrganizations',
    Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'ID',
            LocalDataProperty : salesOrganization_ID
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'salesOrganization',
            ![@UI.Importance] : #High
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'salesOrganizationName',
            ![@UI.Importance] : #High
        },
    ]
});
