using {com.sap.ic.cmh as cmh} from '../../../db/cds/index';

service CostCollectorService {

    entity ItemTypes      as projection on cmh.itemType.ItemTypes;
    entity SubItemTypes   as projection on cmh.subItemType.SubItemTypes;
    entity UnitOfMeasures  as projection on cmh.unitOfMeasure.UnitOfMeasures;
    entity ServiceMaterialDestinations as projection on cmh.serviceMaterialDestination.ServiceMaterialDestinations;
    entity ServiceMaterials     as projection on cmh.serviceMaterial.ServiceMaterials;
    entity SubItemTypesUnits as projection on cmh.subItemTypeUnit.SubItemTypesUnits;

    @Capabilities.Deletable : false
    entity Complaints     as projection on cmh.complaint.Complaints actions {
        @(
            cds.odata.bindingparameter.name : '_it',

            Common.SideEffects              : {
                TargetEntities   : [_it.costCollector],
                TargetProperties : [
                '_it.totalSubLetCost',
                '_it.totalLaborHour',
                '_it.currency_code',
                '_it.laborUnit_code'
                ]
            },

        )
        action AddFR(  @(Common : {ValueListMapping : {
            Label          : '{i18n>SUB_ITEM_TYPE}',
            CollectionPath : 'SubItemTypesUnits',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : subItemType_code
            },
             {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'itemType_code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'unit_code',
                LocalDataProperty : unit_code
            },
             {
                $Type             : 'Common.ValueListParameterIn',
                ValueListProperty : 'companyCode_ID',
                LocalDataProperty : _it.companyCode_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        subItemType_code : cmh.complaint.Complaints : costCollector.subItemType_code
        @(Common : {FieldControl : #Mandatory})
        @label                    : '{i18n>SUB_ITEM_TYPE}',

        @(Common : {ValueListMapping : {
            Label          : '{i18n>ITEM_TYPE}',
            CollectionPath : 'ItemTypes',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        itemType_code : cmh.complaint.Complaints : costCollector.itemType_code
        @UI.ParameterDefaultValue : 'FR'
        @(Common : {FieldControl : #ReadOnly})
        @label                    : '{i18n>ITEM_TYPE}',
        @title                    : '{i18n>QUANTITY}'
        @(Common : {FieldControl : #Mandatory})
        quantity : cmh.complaint.Complaints : costCollector.quantity,
                       @(Common : {ValueListMapping : {
            Label          : '{i18n>UNIT}',
            CollectionPath : 'ServiceMaterialDestinations',
            Parameters     : [
             {
                $Type             : 'Common.ValueListParameterOut',
                ValueListProperty : 'unit_code',
                LocalDataProperty : unit_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'unit/name'
            },
            {
                $Type             : 'Common.ValueListParameterIn',
                ValueListProperty : 'companyCode_ID',
                LocalDataProperty : _it.companyCode_ID
            },
            {
                $Type             : 'Common.ValueListParameterIn',
                ValueListProperty : 'subItemType_code',
                LocalDataProperty : subItemType_code
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'itemType_code',
                Constant : 'FR'
            }
            ]
        }})
        unit_code  : cmh.complaint.Complaints : costCollector.unit_code
        @(Common : {FieldControl : #Mandatory})
        @label                    : '{i18n>UNIT}',
        @title                    : '{i18n>DESCRIPTION}'
        @UI.MultiLineText
        description : cmh.complaint.Complaints : costCollector.description) returns Complaints;
        @(
            cds.odata.bindingparameter.name : '_it',

            Common.SideEffects              : {
                TargetEntities   : [_it.costCollector],
                TargetProperties : [
                '_it.totalSubLetCost',
                '_it.totalLaborHour',
                '_it.currency_code',
                '_it.laborUnit_code'
                ]
            },

        )

        action AddSUBL(@(Common : {ValueListMapping : {
            Label          : '{i18n>SUB_ITEM_TYPE}',
            CollectionPath : 'SubItemTypes',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : subItemType_code
            },
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'itemType_code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        subItemType_code : cmh.complaint.Complaints : costCollector.subItemType_code
        @(Common : {FieldControl : #Mandatory})
        @label                    : '{i18n>SUB_ITEM_TYPE}',

        @(Common : {ValueListMapping : {
            Label          : '{i18n>ITEM_TYPE}',
            CollectionPath : 'ItemTypes',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        itemType_code : cmh.complaint.Complaints : costCollector.itemType_code
        @UI.ParameterDefaultValue : 'SUBL'
        @(Common : {FieldControl : #ReadOnly, })
        @label                    : '{i18n>ITEM_TYPE}',
        @title                    : '{i18n>TOTAL_COST}'
        @(Common : {FieldControl : #Mandatory})
        totalCost : cmh.complaint.Complaints : costCollector.totalCost,
                       @(Common : {ValueListMapping : {
            Label          : '{i18n>CURRENCY}',
            CollectionPath : 'Currencies',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : currency_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name'
            }
            ]
        }})
        currency_code : cmh.complaint.Complaints : costCollector.currency.code
        @(Common : {FieldControl : #Mandatory})
        @label                    : '{i18n>CURRENCY}',
        @title                    : '{i18n>DESCRIPTION}'
        @UI.MultiLineText
        description : cmh.complaint.Complaints : costCollector.description) returns Complaints;

    }

    entity CostCollectors as projection on cmh.costCollector.CostCollectors actions {
        @(
            cds.odata.bindingparameter.name : '_it',
            Common.SideEffects              : {
                TargetEntities   : [_it.parent],
                TargetProperties : [
                '_it.parent_totalSubLetCost',
                '_it.parent_totalLaborHour',
                '_it.parent_currency_code',
                '_it.laborUnit_code'
                ]
            }


        )
        action EditSUBL(@(Common : {ValueListMapping : {
            Label          : '{i18n>SUB_ITEM_TYPE}',
            CollectionPath : 'SubItemTypes',

            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : subItemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        subItemType_code :      cmh.costCollector.CostCollectors : subItemType_code
        @UI.ParameterDefaultValue : _it.subItemType_code
        @(Common : {FieldControl : #ReadOnly, })
        @label                    : '{i18n>SUB_ITEM_TYPE}',

        @(Common : {ValueListMapping : {
            Label          : '{i18n>ITEM_TYPE}',
            CollectionPath : 'ItemTypes',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        itemType_code : cmh.costCollector.CostCollectors : itemType_code
        @UI.ParameterDefaultValue : _it.itemType_code
        @(Common : {FieldControl : #ReadOnly})
        @label                    : '{i18n>ITEM_TYPE}',
        @title                    : '{i18n>TOTAL_COST}'
        @UI.ParameterDefaultValue : _it.totalCost
        @(Common : {FieldControl : #Mandatory})
        totalCost : cmh.costCollector.CostCollectors : totalCost,
        @(Common : {ValueListMapping : {
            Label          : '{i18n>CURRENCY}',
            CollectionPath : 'Currencies',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : currency_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'name'
            }
            ]
        }})
        currency_code : cmh.costCollector.CostCollectors : currency.code
        @(Common : {FieldControl : #Mandatory})
        @UI.ParameterDefaultValue : _it.currency_code
        @label                    : '{i18n>CURRENCY}',
        @title                    : '{i18n>DESCRIPTION}'
        @UI.ParameterDefaultValue : _it.description
        @UI.MultiLineText
        description : cmh.costCollector.CostCollectors : description) returns CostCollectors;
        @(
            cds.odata.bindingparameter.name : '_it',
            Common.SideEffects              : {
                TargetEntities   : [_it.parent],
                TargetProperties : [
                '_it.parent_totalSubLetCost',
                '_it.parent_totalLaborHour',
                '_it.parent_currency_code',
                '_it.laborUnit_code'
                ]
            }


        )
        action EditFR(  @(Common : {ValueListMapping : {    
            Label          : '{i18n>SUB_ITEM_TYPE}',
            CollectionPath : 'SubItemTypesUnits',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : subItemType_code
            },
             {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'itemType_code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'unit_code',
                LocalDataProperty : unit_code
            },
            {
                $Type             : 'Common.ValueListParameterIn',
                ValueListProperty : 'companyCode_ID',
                LocalDataProperty : _it.companyCode_ID
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        subItemType_code :      cmh.costCollector.CostCollectors : subItemType_code
        @UI.ParameterDefaultValue : _it.subItemType_code
        @(Common : {FieldControl : #ReadOnly, })
        @label                    : '{i18n>SUB_ITEM_TYPE}',

        @(Common : {ValueListMapping : {
            Label          : '{i18n>ITEM_TYPE}',
            CollectionPath : 'ItemTypes',
            Parameters     : [
            {
                $Type             : 'Common.ValueListParameterInOut',
                ValueListProperty : 'code',
                LocalDataProperty : itemType_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'description'
            }
            ]
        }})
        itemType_code : cmh.costCollector.CostCollectors : itemType_code
        @UI.ParameterDefaultValue : _it.itemType_code
        @(Common : {FieldControl : #ReadOnly})
        @label                    : '{i18n>ITEM_TYPE}',
        @title                    : '{i18n>QUANTITY}'
        @UI.ParameterDefaultValue : _it.quantity
        @(Common : {FieldControl : #Mandatory})
        quantity : cmh.costCollector.CostCollectors : quantity,

        @(Common : {ValueListMapping : {
            Label          : '{i18n>UNIT}',
            CollectionPath : 'ServiceMaterialDestinations',
            Parameters     : [
             {
                $Type             : 'Common.ValueListParameterOut',
                ValueListProperty : 'unit_code',
                LocalDataProperty : unit_code
            },
            {
                $Type             : 'Common.ValueListParameterDisplayOnly',
                ValueListProperty : 'unit/name'
            },
            {
                $Type             : 'Common.ValueListParameterIn',
                ValueListProperty : 'companyCode_ID',
                LocalDataProperty : _it.companyCode_ID
            },
            {
                $Type             : 'Common.ValueListParameterIn',
                ValueListProperty : 'subItemType_code',
                LocalDataProperty : subItemType_code
            },
            {
                $Type             : 'Common.ValueListParameterConstant',
                ValueListProperty : 'itemType_code',
                Constant : 'FR'
            }
            ]
        }})
        unit_code : cmh.costCollector.CostCollectors : unit_code
        @(Common : {FieldControl : #Mandatory})
        @UI.ParameterDefaultValue : _it.unit_code
        @label                    : '{i18n>UNIT}',
        @title                    : '{i18n>DESCRIPTION}'
        @UI.ParameterDefaultValue : _it.description
        @UI.MultiLineText
        description : cmh.costCollector.CostCollectors : description) returns CostCollectors;
        @(
            cds.odata.bindingparameter.name : '_it',
            Common.SideEffects              : {
                TargetEntities   : [_it.parent],
                TargetProperties : [
                '_it.parent_totalSubLetCost',
                '_it.parent_totalLaborHour',
                '_it.parent_currency_code',
                '_it.laborUnit_code'
                ]
            },
            Common.IsActionCritical: true
        )
        action Delete() returns CostCollectors;
    }

}