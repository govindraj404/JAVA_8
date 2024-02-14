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


# bosh
https://teams.microsoft.com/l/meetup-join/19%3ameeting_ZWU3Y2FkNDgtNDQ2Yi00MmY5LTlhN2QtOGE3N2FlZGY5ZjRk%40thread.v2/0?context=%7b%22Tid%22%3a%220ae51e19-07c8-4e4b-bb6d-648ee58410f4%22%2c%22Oid%22%3a%2257a84d8a-1c0d-42d5-ab85-9ae906354fc2%22%7d

#TcS id
https://teams.microsoft.com/l/meetup-join/19%3ameeting_YTAzOWE5YjItMjQzZS00MzlmLTllM2YtZDM5MGQ5NzYzMDNk%40thread.v2/0?context=%7b%22Tid%22%3a%22404b1967-6507-45ab-8a6d-7374a3f478be%22%2c%22Oid%22%3a%2295f639e6-c94d-4234-afbe-64793df24131%22%7d

https://teams.microsoft.com/l/meetup-join/19%3ameeting_MzMyMDI3ZDUtM2Y4NC00ZjYyLWE3YTMtZjFjMzI5N2M4OTQ5%40thread.v2/0?context=%7b%22Tid%22%3a%2295119adf-3c99-4cd4-a052-694f60948651%22%2c%22Oid%22%3a%2239a672f8-c81c-4326-a2d1-9bc57459976e%22%7d



