import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  loginForm: FormGroup;

  constructor(
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private http: HttpClient,
    private router: Router
  ) { }

  ngOnInit() {
    this.loginForm = this.fb.group({
      user: ['', [Validators.required]],
      password: ['', [Validators.required]],
      smagentname: [''],
      target: [''],
    });

    // Fetch values from URL parameters using FormData
    const formData = new FormData();
    this.route.queryParams.subscribe(params => {
      formData.set('smagentname', params['smagentname'] || '');
      formData.set('target', params['target'] || '');

      // Patch the form values
      this.loginForm.patchValue({
        smagentname: formData.get('smagentname') || '',
        target: formData.get('target') || '',
      });
    });
  }

  onSubmit() {
    // Prepare data for POST request
    const formData = new FormData();
    formData.set('user', this.loginForm.get('user').value);
    formData.set('password', this.loginForm.get('password').value);
    formData.set('smagentname', this.loginForm.get('smagentname').value);
    formData.set('target', this.loginForm.get('target').value);

    // Set headers for x-www-form-urlencoded content type
    const headers = new HttpHeaders().set('Content-Type', 'application/x-www-form-urlencoded');

    // Make POST request
    this.http.post('https://loginUrl/login.fcc', this.encodeFormData(formData), { headers })
      .subscribe(response => {
        // Handle the response as needed
        console.log(response);
        // Redirect to another page if necessary
        this.router.navigate(['/success']);
      });
  }

  // Helper function to encode form data for x-www-form-urlencoded format
  private encodeFormData(formData: FormData): string {
    const urlSearchParams = new URLSearchParams();
    formData.forEach((value, key) => {
      urlSearchParams.append(key, value as string);
    });
    return urlSearchParams.toString();
  }
}
