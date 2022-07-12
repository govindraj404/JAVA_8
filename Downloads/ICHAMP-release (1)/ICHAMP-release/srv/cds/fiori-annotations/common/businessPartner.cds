using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.businessPartner.BusinessPartners with {

    ID
    @UI.HiddenFilter
    @Common       : {
        Label : '{i18n>BUSINESS_PARTNER_NUMBER}',
        Text  : {
            $value                 : businessPartnerNumber,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    businessPartnerName1
    @Common.Label : '{i18n>BUSINESS_PARTNER_FIRST_NAME}';

    businessPartnerNumber
    @Common.Label : '{i18n>BUSINESS_PARTNER_NUMBER}';

    businessPartnerType
    @Common.Label : '{i18n>BUSINESS_PARTNER_TYPE}';

    businessPartnerType
    @UI.Hidden;

    isMarkedForDeletion
    @UI.Hidden;

    vendorCode
    @UI.HiddenFilter;

    customerCode
    @UI.HiddenFilter;

    businessPartnerName2
    @UI.HiddenFilter
    @Common.Label : '{i18n>BUSINESS_PARTNER_LAST_NAME}';

    companyCodeID
    @UI.HiddenFilter;

    addressID
    @UI.HiddenFilter;
}

annotate cmh.businessPartner.BusinessPartners with @(
    Capabilities.SearchRestrictions.Searchable : true,
    UI.HeaderInfo                              : {
        TypeName       : 'Supplier',
        TypeNamePlural : 'Suppliers',
        Title          : {
            $Type : 'UI.DataField',
            Value : businessPartnerNumber
        },
        Description    : {
            $Type : 'UI.DataField',
            Value : businessPartnerName1
        },
        ImageUrl       : 'sap-icon://offsite-work'
    },
    UI.QuickViewFacets                         : [
        {
            $Type  : 'UI.ReferenceFacet',
            Target : '@Communication.Contact'
        },
        {
            $Type  : 'UI.ReferenceFacet',
            Label  : '{i18n>COMPANY_DETAILS}',
            Target : '@UI.FieldGroup#QuickView'
        }
    ],
    Communication.Contact                      : {
        fn    : businessPartnerName1,
        email : [{
            type    : #work,
            address : addressID.email
        }],
        tel   : [
            {
                uri  : addressID.mobile,
                type : #cell
            },
            {
                uri  : addressID.email,
                type : #fax
            }
        ]
    },
    UI.FieldGroup #QuickView                   : {Data : [
        {
            $Type : 'UI.DataField',
            Value : addressID.postalCode,
        },
        {
            $Type : 'UI.DataField',
            Value : addressID.mobile,
        },
        {
            $Type : 'UI.DataField',
            Value : addressID.email,
        }
    ]}
);
