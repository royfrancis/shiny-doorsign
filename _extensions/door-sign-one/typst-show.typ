#show: door-sign-one.with(

  $if(logo-left)$
  logo-left: "$logo-left$",
  $endif$

  $if(logo-right)$
  logo-right: "$logo-right$", 
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

  $if(tracking)$
  tracking: $tracking$,
  $endif$

  // person info
  $if(person-1.name)$
  person-1-name: "$person-1.name$",
  $endif$

  $if(person-1.content)$
  person-1-content: [$person-1.content$],
  $endif$

  $if(person-1.profile)$
  person-1-profile: "$person-1.profile$",
  $endif$

  )
