#set text(font: "Lato", fill: rgb("#444444"))
#set par(leading: 0.8em)
#set block(spacing: 1.4em)

#let door-sign-one(

  logo-left: none,
  logo-right: none,
  name: none,
  main-content: none,
  profile: none,
  profile-height: none,
  font-size: 16pt,
  gap-above-profile: 2cm,
  gap-below-profile: 0.5cm,
  body

) = {

  // body font.
  set text(font-size, font: "Lato")

  // set document(title: title)

  set page(
    paper: "a4",
    flipped: true,
    margin: (left: 1cm, right: 16cm, top: 3.5cm, bottom: 1.5cm),
    // background: rect(height: 100%, width: 100%, fill: rgb("#FFCBC4")),
    header: grid(
      columns: (1fr, 1fr),
      row-gutter: 0pt,
      align(left, image(logo-left.path, height: 9mm)),
      align(right, image(logo-right.path, height: 9mm)),
    )
  )

  // page body
  grid(
    columns: (1fr),
    row-gutter: 20pt,

    if profile != none {
      pad(
        box(clip: true, stroke: 0pt, radius: profile-height,
          width: profile-height, height: profile-height,
          image(profile.path, height: profile-height)
        ),
        top: gap-above-profile, bottom: gap-below-profile)
    },
    
    text(1.3em, tracking: 1pt, weight: 600, name),

    // content
    text(1em, tracking: 0.5pt, main-content),

    // embellishment
    // place(left, rect(width: 1cm, height: 1cm, fill: rgb("#BDD775"))),

    // body flow
    {
      body
    }
  )
}
