import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';

@Component({
  selector: 'app-your-component',
  templateUrl: './your-component.component.html',
  styleUrls: ['./your-component.component.css']
})
export class YourComponent implements OnInit {
  myForm: FormGroup;

  constructor(private formBuilder: FormBuilder) { }

  ngOnInit() {
    this.myForm = this.formBuilder.group({
      radioOption: [''] // No need for Validators.required in this case
    });

    // Subscribe to changes in the radioOption control
    this.myForm.get('radioOption').valueChanges.subscribe(value => {
      if (value === 'yes') {
        this.myForm.get('radioOption').patchValue({ 'no': false }, { emitEvent: false });
      } else if (value === 'no') {
        this.myForm.get('radioOption').patchValue({ 'yes': false }, { emitEvent: false });
      }
    });
  }
}
