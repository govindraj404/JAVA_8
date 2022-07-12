using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.companyCode.CompanyCodes with {
     ID
     @UI.Hidden : true
     @Common : {
        Label                    : '{i18n>COMPANY_CODE}',
        Text  : {
            $value                : companyCode,
            ![@UI.TextArrangement] : #TextOnly
            }
    };	
     plants
     @UI.HiddenFilter;
     country
     @UI.HiddenFilter;
     currency
     @UI.HiddenFilter;

     addressID_ID
     @UI.Hidden;

     addressID
     @UI.HiddenFilter;

     address
     @UI.Hidden;

     countryKey
     @UI.HiddenFilter;
     
     companyCode
     @Common.Label : '{i18n>COMPANY_CODE}';

    companyCodeName
    @Common.Label  : '{i18n>NAME}';

 };

annotate cmh.companyCode.CompanyCodes with @(
    Capabilities.SearchRestrictions.Searchable : true,
    UI: {
        SelectionFields                  : [
       companyCode,companyCodeName,addressID.city
        ]
    }
);

annotate cmh.companyCode.CompanyCode with @(
    Common.ValueListMapping : {
        Label          : '{i18n>COMPANY_CODE}',
        CollectionPath : 'CompanyCodes',
        Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'ID',
                LocalDataProperty : companyCode_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'companyCode',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'companyCodeName',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'addressID/city',
                ![@UI.Importance] : #High
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'currency/code',
                ![@UI.Importance] : #High
            }
        ]
    }
);