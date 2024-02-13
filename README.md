wipro

https://www.flocareer.com/dynamic/t/LRPrvu

```typescript

import { Component, Inject } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { DOCUMENT } from '@angular/common';
import { AuthService } from './auth.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  loginForm: FormGroup;
  domainUrl: string;

  constructor(
    private formBuilder: FormBuilder,
    private authService: AuthService,
    @Inject(DOCUMENT) private document: Document
  ) {}

  ngOnInit() {
    // Create the form dynamically with form controls
    this.loginForm = this.formBuilder.group({
      username: ['', Validators.required],
      password: ['', Validators.required],
      // Add other form controls as needed
    });

    // Get the domain URL from the current location
    this.domainUrl = this.extractDomainFromUrl(this.document.location.href);

    // Dynamically set the action attribute of the form to the domain URL
    const formElement = this.document.querySelector('form');

    if (formElement) {
      formElement.setAttribute('action', this.domainUrl);
    }
  }

  submitForm() {
    this.authService.login(this.loginForm).subscribe(
      response => {
        console.log('Login successful:', response);
        // Handle the server response as needed.
      },
      error => {
        console.error('Login failed:', error);
        // Handle login failures appropriately.
      }
    );
  }

  private extractDomainFromUrl(url: string): string {
    const urlObject = new URL(url);
    const port = urlObject.port ? `:${urlObject.port}` : '';
    return `${urlObject.protocol}//${urlObject.hostname}${port}`;
  }
}

