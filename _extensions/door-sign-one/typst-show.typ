#show: door-sign-one.with(

  $if(logo-left)$
  logo-left: (
    path: "$logo-left.path$"
  ), 
  $endif$

  $if(logo-right)$
  logo-right: (
    path: "$logo-right.path$"
  ), 
  $endif$

  $if(font-size)$
  font-size: $font-size$,
  $endif$

  $if(profile-height)$
  profile-height: $profile-height$,
  $endif$

  $if(gap-above-profile)$
  gap-above-profile: $gap-above-profile$,
  $endif$

  $if(gap-below-profile)$
  gap-below-profile: $gap-below-profile$,
  $endif$

  // person info
  $if(person-1.name)$
  name: "$person-1.name$",
  $endif$

  $if(person-1.content)$
  main-content: [$person-1.content$],
  $endif$

  $if(person-1.profile)$
  profile: (
    path: "$person-1.profile.path$"
  ),
  $endif$

  )
