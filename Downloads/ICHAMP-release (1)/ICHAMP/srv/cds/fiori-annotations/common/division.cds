using com.sap.ic.cmh as cmh from '../../../../db/cds/index';

@cds.autoexpose
annotate cmh.division.Divisions with {

    ID
    @UI.Hidden    : true
    @Common       : {
        Label : '{i18n>ID}',
        Text  : {
            $value                 : salesDivisionName,
            ![@UI.TextArrangement] : #TextOnly
        }
    };

    salesDivision
    @Common.Label : '{i18n>DIVISION}';

    salesDivisionName
    @Common.Label : '{i18n>DESCRIPTION}';

    salesOrganizationID
    @UI.HiddenFilter
    @UI.Hidden;

    salesOrganizationID_ID
    @UI.Hidden;

}

annotate cmh.division.Divisions with @(Capabilities.SearchRestrictions.Searchable : true);


annotate cmh.division.Division with
@(Common.ValueListMapping : {
    Label          : '{i18n>DIVISION}',
    CollectionPath : 'Divisions',
    Parameters     : [
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'ID',
            LocalDataProperty : division_ID
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'salesDivision',
            ![@UI.Importance] : #High
        },
        {
            $Type             : 'Common.ValueListParameterDisplayOnly',
            ValueListProperty : 'salesDivisionName',
            ![@UI.Importance] : #High
        },
        {
            $Type             : 'Common.ValueListParameterInOut',
            ValueListProperty : 'salesOrganizationID_ID',
            LocalDataProperty : salesOrganization_ID
        }
    ]
});
