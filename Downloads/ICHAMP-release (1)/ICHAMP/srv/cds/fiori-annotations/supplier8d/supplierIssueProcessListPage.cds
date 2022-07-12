using SupplierIssueProcessService from '../../services/index';

annotate SupplierIssueProcessService.Supplier8DProcesses with @(
    UI : {
        LineItem : [
            {
                $Type             : 'UI.DataField',
                Value             : identifier,
                ![@UI.Importance] : #High
            }
        ]
    }
); 