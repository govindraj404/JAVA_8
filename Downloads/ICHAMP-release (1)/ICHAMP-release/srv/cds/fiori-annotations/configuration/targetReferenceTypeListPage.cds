using ConfigurationService from '../../services/index';

annotate ConfigurationService.TargetReferenceTypes with @(

    Capabilities : {SearchRestrictions.Searchable : false, Deletable : false },
    UI           : {LineItem : {$value : [
        {
            $Type             : 'UI.DataField',
            Value             : targetType_ID,
            ![@UI.Importance] : #High,
            ![@HTML5.CssDefaults] : {width : '10rem'}
        },
        {
            $Type             : 'UI.DataField',
            Label             : '{i18n>TARGET_DOCUMENT_CATEGORY}',            
            Value             : targetType.targetDocumentCategory_code,
            ![@UI.Importance] : #High,
            ![@Common.FieldControl] : #ReadOnly
        },
        {
            $Type             : 'UI.DataField',
            Label             : '{i18n>TARGET_ITEM_CATEGORY}',
            Value             : targetItemCategoryCode,

            ![@UI.Importance] : #High
        },
        {
            $Type             : 'UI.DataField',
            Value             : destinationSystem,
            ![@UI.Importance] : #High
        }
        
    ]}},
    Common.SideEffects #targetType : {
        SourceProperties : [targetType_ID],
        TargetEntities : [
            targetType
        ]
    },
    
);
