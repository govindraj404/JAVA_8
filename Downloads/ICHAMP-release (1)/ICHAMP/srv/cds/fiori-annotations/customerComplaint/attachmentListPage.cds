using CustomerComplaintService from '../../services/index';

annotate CustomerComplaintService.Attachments with @(Common.SideEffects #name : {
    SourceProperties : [name],
    TargetProperties : [
        'modifiedAt',
        'modifiedBy'
    ]
});
