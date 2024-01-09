//#set text(14pt, font: "Lato", fill: rgb("#444444"))
//#set par(leading: 0.6em)
//#set block(spacing: 1.4em)

#let door-sign-one(

  logo-left: none,
  logo-right: none,
  profile-height: none,
  gap-above-profile: 2cm,
  gap-below-profile: 0.6cm,
  font-size: 16pt,
  par-leading: 0.6em,

  person-1-name: none,
  person-1-content: none,
  person-1-profile: none,

  body

) = {

  // body font.
  set text(font-size, font: "Lato", fill: rgb("#444444"))
  // set par(leading: par-leading)
  // set block(spacing: 1.4em)

  // set document(title: title)

  set page(
    paper: "a4",
    flipped: true,
    margin: (left: 1cm, right: 16cm, top: 3.5cm, bottom: 1.5cm),
    // background: rect(height: 100%, width: 50%, fill: rgb("#FFCBC4")),
    background: place(left,
      rect(height: 100%, width: 50%,
            stroke: (paint: silver, thickness: 1pt, dash: "dashed"))
    ),

    header: grid(
      columns: (1fr, 1fr),
      row-gutter: 0pt,
      align(left, image(logo-left, height: 9mm)),
      align(right, image(logo-right, height: 9mm)),
    )
  )
  // page body
  pad(top: gap-above-profile,
    stack(dir: ttb, spacing: 1em,

      // profile image
      if person-1-profile != none {
        pad(bottom: gap-below-profile,
          box(clip: true, stroke: 0pt, radius: profile-height,
            width: profile-height, height: profile-height,
            image(person-1-profile, height: profile-height)
          )
        )
      },

      // name
      text(size: 1.4em, tracking: 1pt, weight: 600, person-1-name),

      // content
      text(size: 1em, tracking: 0.5pt, person-1-content)
    )
  )

  // embellishment
  // place(left, rect(width: 1cm, height: 1cm, fill: rgb("#BDD775"))),

  // body flow
  {
    body
  }
}
