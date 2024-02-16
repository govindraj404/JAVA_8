<html>

<head></head>

<body>

    <form novalidate="novalidate" action="httplogin.fcc" method="post" id="login-form" accept-charset="UTF-8">

        <div class="js-form-item form-item js-form-type-textfield form-type-textfield js-form-item-user form-item-user">

            <label for="edit-user" class="js-form-required form-required">Username/Email</label>

            <input autocorrect="none" autocapitalize="none" spellcheck="false" autofocus="autofocus"

                placeholder="Username/Email" type="text" id="edit-user" name="user" value="" size="60" maxlength="60"

                class="form-text required form-control" required="required" aria-required="true" />

        </div>
        <div

            class="js-form-item form-item js-form-type-password form-type-password js-form-item-password form-item-password">

            <label for="edit-password" class="js-form-required form-required">Password</label>

            <input type="password" id="edit-password" name="password" size="60" maxlength="128"

                class="form-text required form-control" required="required" aria-required="true" />

        </div>

        <input type="hidden" name="smagentname" value="$" />

        <input type="hidden" name="target" value="$SM$" />

        <div>

            <input type="submit" id="edit-submit" name="op" value="Log in"

                class="button button--primary js-form-submit form-submit btn btn-primary" />

        </div>

    </form>

</body>

</html>
