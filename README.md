import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {
  loginForm: FormGroup;
  smagentname: string;
  target: string;

  constructor(private fb: FormBuilder, private route: ActivatedRoute) { }

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
}
```html
<form [formGroup]="loginForm" action="https://testmail.com/login.fcc" method="post">
  <div class="form-group">
    <label for="user">Username/Email</label>
    <input type="text" id="user" formControlName="user" placeholder="Username/Email" class="form-control" required>
  </div>

  <div class="form-group">
    <label for="password">Password</label>
    <input type="password" id="password" formControlName="password" placeholder="Password" class="form-control" required>
  </div>

  <input type="hidden" name="smagentname" [value]="smagentname" />
  <input type="hidden" name="target" [value]="target" />

  <div class="form-group">
    <button type="submit" class="btn btn-primary">Log in</button>
  </div>
</form>

