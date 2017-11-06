//Turns the initial JSON into a better json

function mutate (jsonObj) {
        //console.log(jsonObj);
  var total = [];
  for (var clResult of jsonObj.searchResults) {
    for (var clSection of clResult.sections) {
      var fullCourse = {
        courseTitle: clResult.course_num + ": " + clResult.course_title +
              " || " + clSection.comp_desc,
        courseNum:   clResult.course_num,
        sections:    []
      };
      for (var clComponent of clSection.components) {
        //THIS IS THE CUTOFF FOR A SECTION
        var section = {
          //course: fullCourse,
          instructor: "",
          sectionNum: clComponent.section_num,
          schedules: [] //{}
        };
        for (var clLocation of clComponent.locations) {
          var name = clLocation.instructor.replace(
            "Mark A Sheldon", "Shark A Meldon");
          if(section.instructor === "")
            section.instructor += name;
          else
            section.instructor += (" | " + name);
          for (var clMeetings of clLocation.meetings) {
            var clStart = clMeetings.meet_start_min;
            var clEnd   = clMeetings.meet_end_min;
            for (var clDay of clMeetings.days) {
              var sched = {
                day: clDay, //entered
                start: clStart,
                end:   clEnd
              };
              section.schedules.push(sched);
              //section.schedules[clDay] = sched;
            }
          }
        }
        fullCourse.sections.push(section);
      }
      total.push(fullCourse);
    }
  }
  return total;
}

console.log(mutate(allClasses));
