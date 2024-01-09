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

  // person info 1
  $if(person-1.name)$
  person-1-name: "$person-1.name$",
  $endif$

  $if(person-1.content)$
  person-1-content: [$person-1.content$],
  $endif$

  $if(person-1.profile)$
  person-1-profile: "$person-1.profile$",
  $endif$

  // person info 2
  $if(person-2.name)$
  person-2-name: "$person-2.name$",
  $endif$

  $if(person-2.content)$
  person-2-content: [$person-2.content$],
  $endif$

  $if(person-2.profile)$
  person-2-profile: "$person-2.profile$",
  $endif$

  // person info 3
  $if(person-3.name)$
  person-3-name: "$person-3.name$",
  $endif$

  $if(person-3.content)$
  person-3-content: [$person-3.content$],
  $endif$

  $if(person-3.profile)$
  person-3-profile: "$person-3.profile$",
  $endif$

  // person info 4
  $if(person-4.name)$
  person-4-name: "$person-4.name$",
  $endif$

  $if(person-4.content)$
  person-4-content: [$person-4.content$],
  $endif$

  $if(person-4.profile)$
  person-4-profile: "$person-4.profile$",
  $endif$

  )
