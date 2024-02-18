
```typescript
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';
import { HttpClient } from '@angular/common/http';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  loginForm: FormGroup;
  smagentname: string;
  target: string;

  constructor(
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private http: HttpClient
  ) { }

  ngOnInit(): void {
    this.route.queryParams.subscribe(params => {
      this.smagentname = params['smagentname'] || '$'; // Default value is '$' if not present
      this.target = params['target'] || '$SM$'; // Default value is '$SM$' if not present
      this.initForm();
    });
  }

  initForm(): void {
    this.loginForm = this.fb.group({
      user: ['', [Validators.required]],
      password: ['', [Validators.required]],
    });
  }

  onSubmit(): void {
    if (this.loginForm.valid) {
      const formData = {
        user: this.loginForm.get('user').value,
        password: this.loginForm.get('password').value,
        smagentname: this.smagentname,
        target: this.target
      };

      // Make an HTTP POST request to the desired URL
      this.http.post('https://testmail.com/login.fcc', formData)
        .subscribe(response => {
          // Handle the response from the server if needed
          console.log('Server response:', response);
        });
    }
  }
}

```html
<!-- login.component.html -->

<form action="https://testmail.com/login.fcc" method="post">
  <div class="form-group">
    <label for="user">Username/Email</label>
    <input type="text" id="user" name="user" placeholder="Username/Email" class="form-control" required>
  </div>

  <div class="form-group">
    <label for="password">Password</label>
    <input type="password" id="password" name="password" placeholder="Password" class="form-control" required>
  </div>

  <input type="hidden" name="smagentname" value="{{ smagentname }}" />
  <input type="hidden" name="target" value="{{ target }}" />

  <div class="form-group">
    <button type="submit" class="btn btn-primary">Log in</button>
  </div>
</form>
https://ibegin.tcs.com/iBegin/

