using CostCollectorService from '../../services/index';

annotate CostCollectorService.Complaints with @(
    Capabilities : {FilterRestrictions : {

        NonFilterableProperties      : [
        note,
        description,
        creationType,
        contactPerson_ID,
        complaintType_code,
        material_ID,
        plant_ID,
        supplier_ID
        ],
        FilterExpressionRestrictions : [
        {
            Property           : createdAt,
            AllowedExpressions : 'SingleRange'
        },
        {
            Property           : modifiedAt,
            AllowedExpressions : 'SingleRange'
        }
        ]
    }

    },
    UI           : {
        PresentationVariant : {Visualizations : ['@UI.LineItem']

        },
        SelectionFields     : [
        complaintStatus_code,
        referenceNumber,
        supplier_ID,
        material_ID,
        plant_ID,
        personResponsible_ID,
        createdAt,
        businessObjects.businessObjectID
        ],
        LineItem            : [
        {
            $Type : 'UI.DataField',
            Value : identifier
        },
        {
            $Type : 'UI.DataField',
            Value : complaintStatus_code
        },
        {
            $Type : 'UI.DataField',
            Value : referenceNumber
        },
        {
            $Type : 'UI.DataField',
            Value : supplier_ID
        },
        {
            $Type : 'UI.DataField',
            Value : material_ID
        },
        {
            $Type : 'UI.DataField',
            Value : plant_ID
        },
        {
            $Type : 'UI.DataField',
            Value : personResponsible_ID
        }
        ]
    }
);
